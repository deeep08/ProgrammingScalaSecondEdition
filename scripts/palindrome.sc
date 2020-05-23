val list = List(1,2,3,2,1)

def isPalindrome[A](list: List[A]): Boolean = {
  list == list.reverse
}

println(isPalindrome(list))

