#lang pyret

data Dat:
  | dat(x :: Number)
end

fun f(d :: Dat) -> Number:
  cases (Dat) d:
    | dat(x) => x
  end
end

print(f(dat(8)))
