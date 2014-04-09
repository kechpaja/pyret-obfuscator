#lang pyret


O000O = "No Fields"
O0O0O = "str"
data O0O:   | OOO   | O00O(OO0O :: String) end 
fun O0OO(OOOO :: O0O) -> String:   cases (O0O) OOOO:     | OOO() =>         O000O
    | O00O(OO00O) =>         OO00O
  end
end
print(O0OO(OOO))
print(O0OO(O00O(O0O0O)))
