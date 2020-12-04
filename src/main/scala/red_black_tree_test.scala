object red_black_tree_test extends App{
    val brt = new red_black_tree[Int]
    brt.insert(3)
    brt.insert(2)
    brt.insert(4)
    brt.insert(6)
    brt.insert(5)
//    brt.insert(7)
//    brt.insert(9)
//    brt.insert(8)
//    brt.insert(10)
//    brt.insert(11)
//    brt.insert(12)

    println(brt.searchTree(5))
    println(brt.searchTree(16))
    println(brt.searchTree(3))

    println(brt.searchTree(11))
    println(brt.searchTree(14))



    (brt.printTree())
    println(brt.sortedArray())



}
