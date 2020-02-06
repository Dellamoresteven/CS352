def g(x: Int, y: (Int, Boolean) => Int) :Int = 2 + x;
def f(x: Int, y: Boolean): Int = x;
val z = g(4, f);
z