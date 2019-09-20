import org.scalatest.FunSuite

class IntModTest extends FunSuite {
  import IntMod.U32MaxValue;
  import IntMod.U64MaxValue;

  test("int mod prime 1") {
    val n1 = IntMod(11, 7)
    val n2 = IntMod(4, 7)
    assert(n1==n2)
  }

  test("int mod prime 1.5") {
    val n1 = IntMod(10, 7)
    val n2 = IntMod(4, 7)
    assert(n1!=n2)
  }

  test("int mod prime 2") {
    val n1 = IntMod(Long.MinValue, 7);
    val n2 = IntMod(6, 7);
    assert(n1==n2)
  }

  test("int mod prime 3") {
    val n1 = IntMod(Long.MaxValue, 7);
    val n2 = IntMod(0, 7);
    assert(n1==n2)
  }

  test("int mod prime 4") {
    val n1 = IntMod(-1, 7);
    val n2 = IntMod(6, 7);
    assert(n1==n2)
  }

  test("int mod prime 5") {
    val n1 = IntMod(-23, 7);
    val n2 = IntMod(5, 7);
    assert(n1==n2)
  }

  test("pow mod 1") {
    val n: BigInt = 4;
    val result = n.modPow(2, 3);
    assert(result==1)
  }

  test("pow mod 2") {
    val n: BigInt = 34;
    val result = n.modPow(456, 13);
    assert(result==1)
  }

  test("pow mod 3") {
    val n: BigInt = 12563;
    val result = n.modPow(69521, 57);
    assert(result==17)
  }

  test("pow mod 4") {
    val n: BigInt = 54;
    val result = n.modPow(U64MaxValue, 59);
    assert(result==6)
  }

  test("pow mod 5") {
    val n: BigInt = 54;
    val result = n.modPow(U32MaxValue, 59);
    assert(result==8)
  }

  test("pow mod 6") {
    val result = U32MaxValue.modPow(2, U32MaxValue);
    assert(result==0)
  }

  test("pow mod 7") {
    val result = U32MaxValue.modPow(2, U32MaxValue - 1);
    assert(result==1)
  }

  test("pow mod 8") {
    val result = U32MaxValue.modPow(8, U32MaxValue);
    assert(result==0)
  }

  test("pow mod 9") {
    val result = U32MaxValue.modPow(8, U32MaxValue - 2);
    assert(result==256)
  }

  test("pow mod 10") {
    val n: BigInt = 4294967284L;
    val result = n.modPow(8, 21023);
    assert(result==2576)
  }

  test("pow mod 11") {
    val n: BigInt = 4294967284L;
    val result = n.modPow(8, 19753);
    assert(result==4129)
  }

  test("pow mod 12") {
    val n: BigInt = 4294967284L;
    val result = n.modPow(3, 21023);
    assert(result==18394)
  }

  test("pow mod 13") {
    val n: BigInt = 4294967284L;
    val result = n.modPow(3, 19753);
    assert(result==18990)
  }

  test("pow mod 14") {
    val n: BigInt = 4294967284L;
    val result = n.modPow(7, 4294957081L);
    assert(result==252464865)
  }

  test("pow mod 15") {
    val n: BigInt = 4294967284L;
    val result = n.modPow(7, 4294956217L);
    assert(result==160775597)
  }

  test("add 1") {
    val n1 = IntMod(5, 7);
    val n2 = IntMod(3, 7);
    val result = n1 + n2;
    assert(result==IntMod(1, 7))
  }

  test("add 2") {
    val n1 = IntMod(9, 11);
    val n2 = IntMod(4, 11);
    val result = n1 + n2;
    assert(result==IntMod(2, 11))
  }
  test("add 3") {
    val n1 = IntMod(U32MaxValue, 7);
    val n2 = IntMod(U32MaxValue, 7);
    val result = n1 + n2;
    assert(result==IntMod(6, 7))
  }

  test("add 4") {
    val n1 = IntMod(Long.MinValue, 7);
    val n2 = IntMod(Long.MinValue, 7);
    val result = n1 + n2;
    assert(result==IntMod(5, 7))
  }

  test("add 5") {
    val n1 = IntMod(Long.MaxValue, 7);
    val n2 = IntMod(Long.MaxValue, 7);
    val result = n1 + n2;
    assert(result==IntMod(0, 7))
  }

  test("sub 1") {
    val n1 = IntMod(5, 7);
    val n2 = IntMod(3, 7);
    val result = n1 - n2;
    assert(result==IntMod(2, 7))
  }

  test("sub 2") {
    val n1 = IntMod(9, 7);
    val n2 = IntMod(4, 7);
    val result = n1 - n2;
    assert(result==IntMod(5, 7))
  }

  test("sub 3") {
    val n1 = IntMod(U32MaxValue, 7);
    val n2 = IntMod(U32MaxValue, 7);
    val result = n1 - n2;
    assert(result==IntMod(0, 7))
  }

  test("sub 4") {
    val n1 = IntMod(Long.MinValue, 7);
    val n2 = IntMod(Long.MinValue, 7);
    val result = n1 - n2;
    assert(result==IntMod(0, 7))
  }

  test("sub 5") {
    val n1 = IntMod(Long.MaxValue, 7);
    val n2 = IntMod(Long.MaxValue, 7);
    val result = n1 - n2;
    assert(result==IntMod(0, 7))
  }

  test("sub 6") {
    val n1 = IntMod(Long.MaxValue, 7);
    val n2 = IntMod(Long.MinValue, 7);
    val result = n1 - n2;
    assert(result==IntMod(1, 7))
  }

  test("sub 7") {
    val n1 = IntMod(Long.MinValue, 7);
    val n2 = IntMod(Long.MaxValue, 7);
    val result = n1 - n2;
    assert(result==IntMod(6, 7))
  }

  test("sub 8") {
    val n1 = IntMod(3, 7);
    val n2 = IntMod(6, 7);
    val result = n1 - n2;
    assert(result==IntMod(4, 7))
  }

  test("mul 1") {
    val n1 = IntMod(5, 7);
    val n2 = IntMod(3, 7);
    val result = n1 * n2;
    assert(result==IntMod(1, 7))
  }

  test("mul 2") {
    val n1 = IntMod(9, 11);
    val n2 = IntMod(4, 11);
    val result = n1 * n2;
    assert(result==IntMod(3, 11))
  }

  test("mul 3") {
    val n1 = IntMod(U32MaxValue, 7);
    val n2 = IntMod(U32MaxValue, 7);
    val result = n1 * n2;
    assert(result==IntMod(2, 7))
  }

  test("mul 4") {
    val n1 = IntMod(Long.MinValue, 7);
    val n2 = IntMod(Long.MinValue, 7);
    val result = n1 * n2;
    assert(result==IntMod(1, 7))
  }

  test("mul 5") {
    val n1 = IntMod(Long.MaxValue, 7);
    val n2 = IntMod(Long.MaxValue, 7);
    val result = n1 * n2;
    assert(result==IntMod(0, 7))
  }

  test("mul 6") {
    val n1 = IntMod(Long.MaxValue, 7);
    val n2 = IntMod(Long.MinValue, 7);
    val result = n1 * n2;
    assert(result==IntMod(0, 7))
  }

  test("mul 7") {
    val n1 = IntMod(Long.MinValue, 7);
    val n2 = IntMod(Long.MaxValue, 7);
    val result = n1 * n2;
    assert(result==IntMod(0, 7))
  }

  test("mul 8") {
    val n1 = IntMod(3, 7);
    val n2 = IntMod(6, 7);
    val result = n1 * n2;
    assert(result==IntMod(4, 7))
  }

  test("div 1") {
    val n1 = IntMod(3, 7);
    val n2 = IntMod(6, 7);
    val result = n1 / n2;
    assert(result==IntMod(4, 7))
  }

  test("pow 1") {
    val n = IntMod(3, 7);
    val result = n.pow(2);
    assert(result==IntMod(2, 7))
  }

  test("pow 2") {
    val n = IntMod(3, 7);
    val result = n.pow(4);
    assert(result==IntMod(4, 7))
  }

  test("pow 3") {
    val n = IntMod(3, 7);
    val result = n.pow(-1);
    assert(result==IntMod(5, 7))
  }

  test("pow 4") {
    val n = IntMod(6, 7);
    val result = n.pow(-2);
    assert(result==IntMod(1, 7))
  }

  test("pow 5") {
    val n = IntMod(7, 11);
    val result = n.pow(Long.MaxValue);
    assert(result==IntMod(6, 11))
  }

  test("pow 6") {
    val n = IntMod(7, 11);
    val result = n.pow(Long.MinValue);
    assert(result==IntMod(5, 11))
  }

}
