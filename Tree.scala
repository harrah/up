/*import Bool._
import TList._

sealed trait Tree
{ tree =>
	type Key
	type Value
	type Root <: Node
	type Ord[a <: Key, b <: Key] <: Comparison

	type Contains[K <: Key] = Root#Contains[K]
	type Get[K <: Key] = Root#Get[K]
	type New[NewRoot <: Node] = Tree {
		type Key = tree.Key
		type Value = tree.Value
		type Root = NewRoot
		type Ord[a <: Key, b <: Key] = tree.Ord[a, b]
	}
	type Put[k <: Key, v <: Value ] = New[Root#Put[k, v]]
	type Delete[k <: Key] = New[Root#Delete[k]]
	type First  = Root#First[Nothing]
	type Last = Root#Last[Nothing]
	type Inorder = Root#Inorder[EmptyTraversal]
	type Postorder = Root#Postorder[EmptyTraversal]
	type Preorder = Root#Preorder[EmptyTraversal]

	type EmptyTraversal = TNil[Pair[Key,Value]]
	type Traversal = TList { type Value = Pair[Key, tree.Value] }
	type Removed = Triple[Node, Key,Value]

	sealed trait Node
	{
		type Contains[k <: Key] <: Bool
		type Put[k <: Key, v <: Value ] <: Node
		type Get[k <: Key] <: Value
		type Delete[k <: Key] <: Node
		type First[Current <: Pair[Key, Value]] <: Pair[Key, Value]
		type Last[Current <: Pair[Key, Value]] <: Pair[Key, Value]
		type DeleteLast[NotLast[R <: Removed] <: Up, IfLast <: Up, Up]<: Up
		type Inorder[T <: Traversal] <: Traversal
		type Postorder[T <: Traversal] <: Traversal
		type Preorder[T <: Traversal] <: Traversal
	}
	sealed trait Branch extends Node
	{ b =>
		type Left <: Node
		type Right <: Node
		type k <: Key
		type v <: Value

		type Get[K <: Key] =
			Ord[K, k]#Match[ Left#Get[K], v, Right#Get[K], Value]
		type Contains[K <: Key] = 
			Ord[K, k]#Match[ Left#Contains[K], True, Right#Contains[K], Bool]
		type Put[K <: Key, V <: Value ] =
			Ord[K, k]#Match[
				Branch { type Left = b.Left#Put[K, V]; type Right = b.Right; type k = b.k; type v = b.v },
				Branch { type Left = b.Left; type Right = b.Right; type k = K; type v = V },
				Branch { type Left = b.Left; type Right = b.Right#Put[K, V]; type k = b.k; type v = b.v },
				Node ]
		type Delete[K <: Key] =
			Ord[K, k]#Match[
				Branch { type Left = b.Left#Delete[K]; type Right = b.Right; type k = b.k; type v = b.v },
				Left#DeleteLast[ DL, Right, Node],
				Branch { type Left = b.Left; type Right = b.Right#Delete[K]; type k = b.k; type v = b.v },
				Node]
		type DL[R <: Removed] = Branch { type k = R#_2; type v = R#_3; type Left = R#_1; type Right = b.Right }


		type First[Current <: Pair[Key, Value]] = Left#First[ThisPair]
		type Last[Current <: Pair[Key, Value]] = Right#Last[ThisPair]
		type ThisPair = Pair[Key, Value] { type _1 = k; type _2 = v }

		type Preorder[T <: Traversal] = ThisPair :: Left#Preorder[ Right#Preorder[ T ] ]
		type Postorder[T <: Traversal] = Left#Preorder[ Right#Preorder[ ThisPair :: T ] ]
		type Inorder[T <: Traversal] = Left#Inorder[ ThisPair :: Right#Inorder[T] ]

		type DeleteLast[NotLast[R <: Removed] <: Up, IfLast <: Up, Up] = NotLast[Right#DeleteLast[ DeleteNotLast, Removed { type _1 = Left; type _2 = k; type _3 = v }, Removed]]
		type DeleteNotLast[R <: Removed] = Removed {
			type _3 = R#_3
			type _2 = R#_2
			type _1 = Branch { type Left = b.Left; type Right = R#_1; type k = b.k; type v = b.v }
		}
	}
	sealed trait Empty extends Node
	{
		type Contains[K <: Key] = False
		type Get[K <: Key] = Nothing
		type Put[K <: Key, V <: Value ] = Branch {
			type k = K
			type v = V
			type Left = Empty
			type Right = Empty
		}
		type Delete[k <: Key] = Empty
		type First[Current <: Pair[Key, Value]] = Current
		type Last[Current <: Pair[Key, Value]] = Current
		type Inorder[T <: Traversal] = T
		type Preorder[T <: Traversal] = T
		type Postorder[T <: Traversal] = T
		type DeleteLast[NotLast[R <: Removed] <: Up, IfLast <: Up, Up] = IfLast
	}
}
object Tree
{
	type empty[K, V, ord[A <: K, B <: K]] = Tree {
		type Key = K
		type Value = V
		type Root = Empty
		type Ord[a <: Key, b <: Key] = ord[a,b]
	}
}*/