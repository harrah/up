package up


/*import Dense._
import Bool._

object TreeTest
{
	type E = Tree.empty[Dense, Bool, Compare]
	type A = E#Put[_3, True]#Put[_5, False]#Put[_1, True]#Put[_14, True]
	type B = A#Delete[_3]#Put[_9, False]#Put[_8, A#Get[_14]]#Delete[_5]
	type C = B#Put[_15, B#Contains[_9]]#Delete[_1]#Delete[_5]#Put[_14, False]

	val false = toBoolean[ E#Contains[_0] ]
	val false = toBoolean[ E#Contains[_1] ]
	val false = toBoolean[ E#Contains[_2] ]
	val true =  toBoolean[ A#Contains[_3] && A#Contains[_5] && A#Contains[_1] && A#Contains[_14] ]
	val true =  toBoolean[ A#Get[_3] && Not[A#Get[_5]] && A#Get[_1] && A#Get[_14] ]
	val true =  toBoolean[ Not[B#Contains[_3]] && Not[B#Contains[_5]] && B#Contains[_1] && B#Contains[_14] && B#Contains[_9] && B#Contains[_8] ]
	val true =  toBoolean[ Not[C#Contains[_3]] && Not[C#Contains[_5]] && Not[C#Contains[_1]] && C#Contains[_14] && C#Contains[_9] && C#Contains[_8] && C#Contains[_15] ]
	val true =  toBoolean[ C#Get[_15] && C#Get[_8] && Not[ C#Get[_14] ] ]

	type LastA = A#Last
	type FirstB = B#First
	type List = C#Inorder
	def run()
	{
		println( toInt[ LastA#_1] + " -> " +  toBoolean[ LastA#_2] )
		println( toInt[ FirstB#_1] + " -> " +  toBoolean[ FirstB#_2] )
		println( toInt[ List#Head#_1 ] + ", " + toInt[ List#Tail#Head#_1 ]  + ", " + toInt[ List#Tail#Tail#Head#_1 ]  + ", " + toInt[ List#Tail#Tail#Tail#Head#_1 ])
	}
}*/