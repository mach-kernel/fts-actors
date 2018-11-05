package util

final case class TreeNode[T](
  data: T,
  var left: Option[TreeNode[T]] = None,
  var right: Option[TreeNode[T]] = None,
  var foundAt: List[Int] = List()
)
