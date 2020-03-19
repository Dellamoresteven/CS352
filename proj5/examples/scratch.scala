// val arr = new Array[Int](14); 
// arr(13) = 8;
// arr(1) = 9;
// arr(13) + arr(1)


// val r = 0.toChar;
// val a = '0'.toInt;
// a

// putchar(65);
// putchar(66);
// putchar(67);
// putchar(68);
// putchar(69);
// getchar()

// 4 << 2

// 6 << 4

// if(23 > 23) {
//     5
// } else {
//     4
// }

// 58 << 4
// 10 << 2
// 30 >> 2
// 58 << 4
// 10 << 2

// 543>>1<<3>>5<<4<<3>>7<<1>>3
// 15%4
// 18%3
// 20 % 8
// -5 * 10
// -4 * -3

// def f(): Int = 5;
// f()

// val k = 1;
// def f(): Int = k;
// f()

// def listMake1[T](e1: T) = e1::Nil;
// def listMake2[T](e1: T, e2: T) = e1::listMake1[T](e2);
// def listMake3[T](e1: T, e2: T, e3: T) = e1::listMake2[T](e2, e3);
// def listForeach[T](f: T => Unit, l: List[T]): Unit =
//   if (!l.isEmpty) {
//     f(l.head);
//     listForeach[T](f, l.tail)
//   };
// def printChar(c: Char) = putchar(c.toInt);
// val l = listMake3[Char]('O', 'K', '\n');
// listForeach[Char](printChar, l);
// 3

def printChar(c: Char) = putchar(c.toInt);
def printString(s: String) = {
  var idx = 0;
  while (idx < s.length) {
    printChar(s(idx));
    idx = idx + 1
  };
  ()
};
printString("OK");
printChar('\n');
3