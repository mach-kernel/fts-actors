package util

trait TreeOperations[T <: Comparable[T]] {
  /**
    * Insert data into the tree
    *
    * @param node
    * @param data
    * @param foundAt
    */
  protected def insertTree(node: TreeNode[T], data: T, foundAt: Int): Unit = {
    if (data == node.data) {
      node.foundAt = foundAt :: node.foundAt
      return
    }

    if (data.compareTo(node.data) > 0) {
      node.right match {
        case Some(child: TreeNode[T]) => insertTree(child, data, foundAt)
        case None => node.right = Some(TreeNode[T](data, foundAt = List(foundAt)))
      }
    } else {
      node.left match {
        case Some(child: TreeNode[T]) => insertTree(child, data, foundAt)
        case None => node.left = Some(TreeNode[T](data, foundAt = List(foundAt)))
      }
    }
  }

  /**
    * Find data in the tree
    *
    * @param node
    * @param data
    * @return
    */
  protected def findItem(node: Option[TreeNode[T]], data: T): Option[TreeNode[T]] = {
    node match {
      case Some(n: TreeNode[T]) => {
        if (data == n.data) return node
        if (data.compareTo(n.data) > 0) findItem(n.right, data) else findItem(n.left, data)
      }
      case None => None
    }
  }
}
