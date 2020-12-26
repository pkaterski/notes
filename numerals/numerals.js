const log = console.log


succ = n => f => x =>
  f (n (f) (x))

id = x =>
  x

zero = _ => x =>
  x

one   =
  succ (zero)

two   =
  succ (one)

three =
  succ (two)

four  =
  succ (three)

five  =
  succ (four)


addOne = x =>
  x + 1

toNumeral = n =>
  n == 0
  ? zero
  : succ (toNumeral (n - 1))

fromNumeral = n =>
  n (addOne) (0)

plus = n => m =>
  n (succ) (m)

compose = f => g => x =>
  f (g (x))

// mult = n => m => f => n (m (f))
// which is the same as composition
// mult = n => m => compose (n) (m)
// and using eta reduction we get:
mult =
  compose

pow = m => n =>
  n (m)


True  = a => _ =>
  a

False = _ => b =>
  b


pair = a => b => select =>
  select (a) (b)

fst = pair =>
  pair (True)

snd = pair =>
  pair (False)


succPair = xs =>
  pair
    (succ (fst (xs)))
    (fst (xs))

pred = n =>
  snd (
    n
      (succPair)
      (pair (zero) (zero)))


// ----------------------------------------------------------------------------------------

showPair = pair =>
  log (`(${ fromNumeral (fst (pair)) }, ${ fromNumeral (snd (pair)) })`)

log ("5 + 3 = " + fromNumeral (plus (five) (three)))
log ("5 * 3 = " + fromNumeral (mult (five) (three)))
log ("5 ^ 3 = " + fromNumeral (pow  (five) (three)))

log ("-1 + 5 ^ 2 = " + fromNumeral (pred  (pow (five) (two))))

ten = plus (five) (five)
log ("-1 + 10 ^ 2 = " + fromNumeral (pred  (pow (ten) (two))))



log ("and now a pair with one & two:")
showPair (pair (one) (two))


