import os
import strutils
import options
import strformat
import sequtils
import rationals

proc permutations[T](arr: seq[T]): seq[seq[T]] =
  if len(arr) <= 0:
    return @[]

  for i, e in arr:
    let newarr = concat(arr[0..(i-1)],
                        arr[(i+1)..(arr.len-1)])
    assert newarr.len == arr.len - 1
    let rperms = permutations(newarr)
    if rperms.len > 0:
      for cand in rperms:
        let r: seq[T] = concat(@[e], cand)
        result.add(r)
    else:
      result.add(@[e])

type
  RNum = Rational[int]

  Binop = enum
    opAdd
    opSub
    opMul
    opDiv

  FormulaKind = enum
    fNum
    fArith

  Formula = ref object
    case kind: FormulaKind
    of fNum:
      value: RNum
    of fArith:
      op: Binop
      left: Formula
      right: Formula

let zero = Rational[int](num:0, den: 1)

# binop methods
proc `$`(op:Binop): string =
  case op
  of opAdd:
    result = "+"
  of opSub:
    result = "-"
  of opMul:
    result = "*"
  of opDiv:
    result = "/"

proc binopCombinations(n: int) : seq[seq[Binop]] =
  if n <= 0:
    result = @[newSeq[Binop]()]
  else:
    result = @[]
    for item in binopCombinations(n-1):
      for op in [opAdd, opSub, opMul, opDiv]:
        let nitem = concat(@[op], item)
        result.add(nitem)

proc `$`(f:Formula): string =
  case f.kind
  of fNum:
    result = $(f.value.toInt)
  of fArith:
    result = fmt"({f.left} {f.op} {f.right})"

proc calcAdd(a: Option[RNum], b: Option[RNum]) : Option[RNum] =
  if a.isSome and b.isSome:
    return some(a.get() + b.get())
  else:
    return none(RNum)

proc calcSub(a: Option[RNum], b: Option[RNum]) : Option[RNum] =
  if a.isSome and b.isSome:
    return some(a.get() - b.get())
  else:
    return none(RNum)

proc calcMul(a: Option[RNum], b: Option[RNum]) : Option[RNum] =
  if a.isSome and b.isSome:
    return some(a.get() * b.get())
  else:
    return none(RNum)

proc calcDiv(a: Option[RNum], b: Option[RNum]) : Option[RNum] =
  if a.isSome and b.isSome:
    if b.get() == zero or (a.get() mod b.get() != zero):
      return none(RNum)
    return some(a.get() / b.get())
  else:
    return none(RNum)

proc calc(f:Formula) : Option[RNum] =
  case f.kind
  of fNum:
    result = some(f.value)
  of fArith:
    case f.op
    of opAdd:
      result = calcAdd(f.left.calc(), f.right.calc())
    of opSub:
      result = calcSub(f.left.calc(), f.right.calc())
    of opMul:
      result = calcMul(f.left.calc(), f.right.calc())
    of opDiv:
      result = calcDiv(f.left.calc(), f.right.calc())

proc buildFormulaTypes(numbers: seq[RNum], ops: seq[Binop]) : seq[Formula] =
  assert numbers.len == 4
  assert ops.len == 3

  let (op0, op1, op2) = (ops[0], ops[1], ops[2])
  proc num(n: int): Formula {.closure, gcsafe.} =
    result = Formula(kind: fNum, value: numbers[n])

  proc arith(left: Formula, op: Binop, right: Formula): Formula {.closure, gcsafe.} =
    result = Formula(kind: fArith, left: left, op: op, right: right)

  result = @[
    arith(
      arith(
        arith(
          num(0), op0, num(1)),
        op1,
        num(2),
      ),
      op2,
      num(3),
    ),

    arith(
      arith(num(0), op0, num(1)),
      op1,
      arith(num(2), op2, num(3)),
    ),

    arith(
      num(0),
      op0,
      arith(num(1),
            op1,
            arith(num(2), op2, num(3)))
    ),
  ]

proc main() =
  let (a, b, c, d) = (paramStr(1), paramStr(2), paramStr(3), paramStr(4))
  let inputs = @[a, b, c, d].map(parseInt).mapIt(Rational[int](num:it, den:1))

  let binopCombs = binopCombinations(3)
  for numbers in permutations(inputs):
    for ops in binopCombs:
      let forTypes = buildFormulaTypes(numbers, ops)
      for f in forTypes:
        let r = f.calc()
        if r.isSome and r.get() == Rational[int](num:24, den:1):
          echo $f
          return

when isMainModule:
  main()
