#lang pyret

data Something:
  | nofields
  | onefield(x :: String)
end


fun f(s :: Something) -> String:
  cases (Something) s:
    | nofields => "No Fields"
    | onefield(x) => x
  end
end

print(f(nofields))
print(f(onefield("str")))
