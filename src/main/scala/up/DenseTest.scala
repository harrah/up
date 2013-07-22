package up


import Bool._
import Dense._
import Comparison._

object DenseTest
{
    val true = toBoolean[ _7#Add[_8]#Compare[_15]#eq]
    val true = toBoolean[ _5#Add[_7]#Compare[_12]#eq]
    val true = toBoolean[ _12#Compare[ _5#Add[_7] ]#eq]
    val true = toBoolean[ _2#Dec#Dec#Add[_11#Dec]#Compare[_10]#eq ]

    val 6561 = toInt[ _9#Sq#Sq ]
    val 1073741824 = toInt[ _4#Exp[_15] ]
    val 279936 = toInt[ _6#Exp[_7] ]

    def run()
    {
        println()
        println( toInt[ _15 ] + ", " + toInt[ _14#Inc ] + ", " + toInt[ _10#Inc#Add[_4] ])
        println(  show[ _13#Add[_11]#Add[_12]#Compare[ _9#Add[_10]#Add[_12] ] ] )
    }
}
