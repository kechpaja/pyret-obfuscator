#lang pyret

data Dat:
  | dat(x :: Number)
end

fun f(d :: Dat) -> Number:
  cases (Dat) d:
    | dat(x) => 
        if x == 8:
          8
        else:
          0
        end
  end
end

print(f(dat(8)))
print(f(dat(9)))
