package util

trait TreeOperations[T] {
  def insert(node: TreeNode[T], data: T, foundAt: Int): Unit
  def findNode(node: TreeNode[T], data: T): Option[TreeNode[T]]
}

object TreeOperations {
  implicit def opsComparable[T <: Comparable[T]]: TreeOperations[T] = new TreeOperations[T] {
    def insert(node: TreeNode[T], data: T, foundAt: Int): Unit = {
      if (data.compareTo(node.data) == 0) {
        node.foundAt = foundAt :: node.foundAt
        return
      }

      if (data.compareTo(node.data) > 0) {
        node.right match {
          case Some(child: TreeNode[T]) => insert(child, data, foundAt)
          case None => node.right = Some(TreeNode[T](data, foundAt = List(foundAt)))
        }
      } else {
        node.left match {
          case Some(child: TreeNode[T]) => insert(child, data, foundAt)
          case None => node.left = Some(TreeNode[T](data, foundAt = List(foundAt)))
        }
      }
    }

    def findNode(node: TreeNode[T], data: T): Option[TreeNode[T]] = {
      if (node.data.compareTo(data) == 0) return Some(node)
      (if (data.compareTo(node.data) > 0) node.right else node.left)
        .flatMap(findNode(_, data))
    }
  }

  implicit class TreeNodeOps[T](node: TreeNode[T])(implicit ops: TreeOperations[T]) {
    def insert(data: T, foundAt: Int): Unit = ops.insert(node, data, foundAt)
    def findNode(data: T): Option[TreeNode[T]] = ops.findNode(node, data)
  }
}
