package chess

import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import cats.syntax.all.*
import Arbitraries.{ *, given }
import org.scalacheck.Prop.propBoolean
import scala.util.Random
import cats.Monoid

class NodeTest extends ScalaCheckSuite:

  import Foo.*

  given HasId[Int, Int] with
    def getId(a: Int): Int = a

  test("mainline size <= node.size"):
    forAll: (node: Node[Int]) =>
      node.mainline.size <= node.size

  test("mainline is a sub set of node"):
    forAll: (node: Node[Int]) =>
      node.mainlineValues.forall(x => node.exists(_ == x))

  test("mainline.size + variations.size == node.size"):
    forAll: (node: Node[Int]) =>
      node.mainline.size + variationsCount(node) == node.size

  test("find with mainline as path retuns last mainline node"):
    forAll: (node: Node[Int]) =>
      node.find(node.mainlineValues).get == node.lastMainlineNode

  test("use mainline as path for findPath"):
    forAll: (node: Node[Int]) =>
      node.findPath(node.mainlineValues) == node.mainline.some

  test("mainlinePath == mainlineValues if A =:= Id"):
    forAll: (node: Node[Int]) =>
      node.mainlineValues == node.mainlinePath

  test("findPath with subset of mainline returns Some"):
    forAll: (node: Node[Int]) =>
      node.mainlineValues.size >= 2 ==> {
        val size = Random.nextInt(node.mainlineValues.size - 1) + 1
        val path = node.mainlineValues.take(size)
        node.findPath(path).isDefined
      }

  test("use randomPath for findPath"):
    forAll: (p: NodeWithPath[Int]) =>
      val (node, path) = p
      path.nonEmpty ==> {
        val found = node.findPath(path).map(_.map(_.value))
        found.isEmpty || found.get == path
      }

  test("with 0 < n <= node.mainline.size => take(n).mainline size == n"):
    forAll: (node: Node[Int]) =>
      val n = Random.nextInt(node.mainline.size)
      n > 0 ==> {
        node.take(n).mainline.size == n
      }

  test("findPath.take(n).mainlineValues isDefined"):
    forAll: (node: Node[Int]) =>
      val n = Random.nextInt(node.mainline.size)
      n > 0 ==> {
        node.findPath(node.take(n).mainlineValues).isDefined
      }

  test("take(node.mainline.size) == node"):
    forAll: (node: Node[Int]) =>
      node.take(node.mainline.size) == node

  test("apply(n) return None if n >= node.mainline.size"):
    forAll: (node: Node[Int]) =>
      node(node.mainline.size).isEmpty

  test("apply(n) return None if n < node.size"):
    forAll: (node: Node[Int]) =>
      val n = if node.mainline.size == 1 then 0 else Random.nextInt(node.mainline.size - 1)
      node(n).isDefined

  test("apply(0) return itself"):
    forAll: (node: Node[Int]) =>
      node(0) == node.some

  test("take(n).size + apply(n).size == node.size"):
    forAll: (node: Node[Int]) =>
      val n = Random.nextInt(node.mainline.size)
      n > 0 ==> {
        node.take(n).size + node(n).map(_.size).getOrElse(0L) == node.size
      }

  test("modifyAt with mainline == modifyLastMainlineNode"):
    forAll: (node: Node[Int], f: Int => Int) =>
      node
        .modifyAt(node.mainlineValues, Tree.liftOption(f)) == node.modifyLastMainlineNode(_.withValue(f)).some

  test("modifyAt and find are consistent"):
    forAll: (p: NodeWithPath[Int]) =>
      val (node, path) = p
      node.modifyAt(path, Tree.liftOption(identity)).isDefined == node.findPath(path).isDefined

  test("modifyAt and modifyChildAt are consistent if the child exists"):
    forAll: (p: NodeWithPath[Int], f: Int => Int) =>
      val (node, path) = p
      def modifyChild(node: Tree[Int]) =
        node.setChild(node.child.map(c => c.withValue(f(c.value)))).some

      node.find(path).flatMap(_.child).isDefined ==> {
        node.modifyAt(path, modifyChild) == node.modifyChildAt(path, _.withValue(f(_)).some)
      }

  test("addValueAsChildOrVariationAt and find are consistent"):
    forAll: (p: NodeWithPath[Foo], foo: Foo) =>
      val (node, ps) = p
      val path       = ps.map(_.id)
      val added      = node.addValueAsChildOrVariationAt(path, foo)
      added.isEmpty || added.flatMap(_.find(path :+ foo.id)).isDefined

  test("addValueAsChildOrVariationAt size"):
    forAll: (p: NodeWithPath[Foo], foo: Foo) =>
      val (node, ps) = p
      ps.nonEmpty ==> {
        val path  = ps.map(_.id)
        val added = node.addValueAsChildOrVariationAt(path, foo)
        added.isEmpty || added.get.size >= node.size
      }

  test("addValueAsChildOrVariationAt twice return the same size"):
    forAll: (p: NodeWithPath[Foo], foo: Foo) =>
      val (node, ps) = p
      ps.nonEmpty ==> {
        val path       = ps.map(_.id)
        val added      = node.addValueAsChildOrVariationAt(path, foo)
        val addedTwice = added.flatMap(_.addValueAsChildOrVariationAt(path, foo))
        added.isEmpty || added.get.size == addedTwice.get.size
      }

  test("mapMainline f . mainlineValues == mainlineValues . f"):
    forAll: (node: Node[Int], f: Int => Int) =>
      node.mapMainline(f).mainlineValues == node.mainlineValues.map(f)

  test("deleteAt with root value return Some(None)"):
    forAll: (node: Node[Int]) =>
      node.deleteAt(List(node.value)) == Some(None)

  test("deleteAt with mainline == deleteLastMainlineNode"):
    forAll: (node: Node[Int]) =>
      node.size >= 2 ==> {
        val deleted = node.deleteAt(node.mainlineValues)
        deleted.isDefined && deleted.get.get.size == node.size - 1
      }

  test("deleteAt and modifyAt are consistent"):
    forAll: (p: (Node[Int], List[Int])) =>
      val (node, path) = p
      node.deleteAt(path).isDefined == node.modifyAt(path, Tree.liftOption(identity)).isDefined

  test("clearVariations.size == mainline.size"):
    forAll: (node: Node[Int]) =>
      node.clearVariations.size == node.mainline.size

  test("clearVariations.mainlineValues == mainlineValues"):
    forAll: (node: Node[Int]) =>
      node.clearVariations.mainlineValues == node.mainlineValues

  test("promote with mainlinePath return none"):
    forAll: (node: Node[Int]) =>
      node.promote(node.mainlineValues).isEmpty

  test("promote with subset of mainline return none"):
    forAll: (node: Node[Int]) =>
      node.mainlineValues.size >= 2 ==> {
        val size = Random.nextInt(node.mainlineValues.size - 1) + 1
        val path = node.mainlineValues.take(size)
        node.promote(path).isEmpty
      }

  test("promote will never change node.size"):
    forAll: (p: NodeWithPath[Int]) =>
      val (node, path) = p
      val ouput        = node.promote(path)
      ouput.isEmpty || ouput.get.size == node.size

  test("promote success reduce mainline nodes count in the path"):
    def nodesInPath(node: Node[Int], path: List[Int]) =
      node.findPath(path).map(_.count(_.isNode)).get

    forAll: (p: NodeWithPath[Int]) =>
      val (node, path) = p
      val ouput        = node.promote(path)
      ouput.isEmpty || {
        nodesInPath(ouput.get, path) == nodesInPath(node, path) + 1
      }

  test("mapAccuml without using accumulator is the same as map"):
    forAll: (node: Node[Int], f: Int => Int) =>
      node.mapAccuml_(0)((s, a) => (s, f(a))) == node.map(f)

  test("mapAccuml is the same as mapAccumlOption"):
    forAll: (node: Node[Int], init: Int, f: (Int, Int) => (Int, Int)) =>
      node.mapAccuml_(init)(f).some == node.mapAccumlOption_(init)((s, a) => f(s, a).map(Some(_)))

  test("mapAccuml is the different thatn mapAccumulate if variation is exist"):
    forAll: (node: Node[Int], n: Int, f: (Int, Int) => (Int, Int)) =>
      val x = node.mapAccuml(n)(f)
      val y = node.mapAccumulate(n)(f)
      node.variationIsEmpty || x != y

  test("mapAccuml with sum"):
    forAll: (node: Node[Int], n: Int) =>
      val s1 = node.mapAccuml_(n.toLong)((s, a) => (s, s + a.toLong)).sumAll
      val s2 = node.foldMap(_.toLong) + node.size * n.toLong
      s1 == s2

  test("variations.add size"):
    forAll: (vs: List[Variation[Foo]], v: Variation[Foo]) =>
      val added       = vs.add(v)
      val isContained = vs.exists(_.value.id == v.value.id)
      added.size == vs.size + (if isContained then 0 else 1)

  test("variations.add keeps the order"):
    forAll: (vs: List[Variation[Foo]], v: Variation[Foo]) =>
      val added       = vs.add(v)
      val isContained = vs.exists(_.sameId(v))
      val expected =
        if isContained then vs.map(_.id)
        else vs.map(_.id) :+ v.id

      added.map(_.id) == expected

  test("variations.add make sure merging is correct"):
    forAll: (vs: List[Variation[Foo]], v: Variation[Foo]) =>
      val added = vs.add(v)
      vs.exists(_.value.id == v.value.id) ==> {
        val orig   = vs.find(_.id == v.id).get
        val output = added.find(_.id == v.id).get
        orig.value.merge(v.value) == output.value
      }

  test("variations.add list"):
    forAll: (vs: List[Variation[Foo]], xs: List[Variation[Foo]]) =>
      val added  = vs.add(xs)
      val allIds = vs.map(_.id).toSet ++ xs.map(_.id).toSet
      added.map(_.id).toSet == allIds

  test("variations.add list size"):
    forAll: (vs: List[Variation[Foo]], xs: List[Variation[Foo]]) =>
      val added       = vs.add(xs)
      val intersected = vs.map(_.id).toSet.intersect(xs.map(_.id).toSet)
      added.map(_.id).toSet.size == vs.map(_.id).toSet.size + xs.map(_.id).toSet.size - intersected.size

  given Monoid[Long] with
    def empty                     = 0L
    def combine(x: Long, y: Long) = x + y

  given Monoid[Int] with
    def empty                   = 0
    def combine(x: Int, y: Int) = x + y

  extension [A](node: Node[A])
    def variationsCount: Long =
      node.child.foldLeft(node.variations.foldMap(_.size))((acc, v) => acc + v.variationsCount)

    def variationIsEmpty: Boolean =
      node.child.foldLeft(node.variations.isEmpty)((acc, v) => acc || v.variationIsEmpty)

  case class Foo(id: Int, name: String)

  object Foo:
    import org.scalacheck.Arbitrary
    import org.scalacheck.Gen

    given Arbitrary[Foo] = Arbitrary:
      for
        id   <- Arbitrary.arbitrary[Int]
        name <- Gen.alphaLowerStr
      yield Foo(id, name)

    given Mergeable[Foo] with
      def merge(x: Foo, y: Foo): Foo = Foo(x.id, x.name + y.name)

    given HasId[Foo, Int] with
      def getId(x: Foo): Int = x.id
